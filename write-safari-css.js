#! /usr/bin/env node

// To get this working after a system restart, I don't have precise instructions
// yet but I'm starting to build up a list:
// 1. Go into Safari's extension builder.
// 2. Remove the Injector extension.
// 3. Add the Injector extension back in.
// 4. Add an injection (right click, inject, save).
// 5. Quit Safari.
// 6. Start Safari back up.
// 7. BEFORE RE-OPENING TABS - run this script - it should succeed.
// 8. Re open tabs.
// 9. Run the extension builder.
// 10. Run the Injector extension.
//
// Usually it fails because either the database file is locked (which means the
// extension must be shut down) or the file doesn't exist (the shm and wal files
// may remain though). If the file doesn't exist, it isn't in a state where it
// would save its values anyways (even with manually providing and saving
// injections).

// NOTE: If the db is ever corrupted by a bad set of 'value', the first record
// is always key = 'StyleStorageVersion', and the value = '1.7.0'. The value is
// also in the same utf16le encoding that the rest of it is.
const fs = require('fs')
const postcss = require('postcss')
const customProperties = require('postcss-custom-properties')
const process = require('process')
const cssVars = require('./css-vars.js')

const readFileData = (path) => {
  try {
    return fs.readFileSync(path, 'utf8')
  }
  // TODO: Specifically look for files not existing as a catch.
  catch(e) {
    console.warn(`File at ${path} not found! Using empty string as content.`)
    return ''
  }
}

const replaceJsVars = (js) => {
  // TODO: Handle error gracefully if I forgot to make this file.
  const vars = require('./js-vars.private.js')
  let result = js
  for(let key in vars) {
    result = result.replace(`process.${key}`, vars[key])
  }
  return result
}

const path = require('path')
const childProcess = require('child_process')
const os = require('os')
const Iconv = require('iconv').Iconv

const tmpDir = path.resolve(__dirname, './.tmp-css-write')
if(!fs.existsSync(tmpDir)) {
  fs.mkdirSync(tmpDir)
}


const names = fs.readdirSync('custom-css')
  .filter(f => f.match(/.+\.json$/))
  .map(f => f.replace(/\.json$/, ''))


const localStorageDir = '~/Library/Safari/LocalStorage'
  .replace('~', os.homedir())

const files = fs.readdirSync(localStorageDir)
const findSqliteFile = /safari-extension_com\.logustus\.injector-(.+?)\.localstorage$/

const sqlitePath = path.join(
  localStorageDir,
  'safari-extension_com.logustus.injector-0000000000_0.localstorage'
  // files.find(f => f.match(findSqliteFile))
)
console.log('Writing to path: ' + sqlitePath)

names.forEach(name => {
  const meta = JSON.parse(fs.readFileSync(
    path.join('custom-css', name + '.json'),
    'utf8'
  ))

  meta.key = meta.key || name

  const css = readFileData(`custom-css/${name}.css`)
  let js = readFileData(`custom-css/${name}.js`)
  if(js == 'undefined') js = ''

  try {
    const customPropPlugin = customProperties()
    customPropPlugin.setVariables(cssVars)
    const processedCss = postcss()
      .use(customPropPlugin)
      .process(css)
      .css

    meta.styles = processedCss
  }
  catch(e) {
    console.error(`error processing file ${name}`, e)
  }

  meta.script = replaceJsVars(js)

  const convertedMeta = new Iconv('UTF-8', 'UTF-16LE')
      .convert(JSON.stringify(meta))

  fs.writeFileSync(`.tmp-css-write/${name}.json`, convertedMeta, 'binary')


// delete from itemtable where key='${meta.key}';
  const updateSql = `
pragma encoding = "UTF-8";
PRAGMA journal_mode=DELETE;
BEGIN EXCLUSIVE;
drop table if exists itemtable;
CREATE TABLE ItemTable (key TEXT UNIQUE ON CONFLICT REPLACE, value BLOB NOT NULL ON CONFLICT FAIL);
insert into itemtable
(key, value) values
('${meta.key}', readfile('.tmp-css-write/${meta.name}.json'))
;
COMMIT;
`

  // console.log(updateSql)
  const sqliteProcess = childProcess.spawnSync('sqlite3', [
    sqlitePath,
    updateSql,
  ])

  // console.log(sqliteProcess.stdout.toString())
  // console.log(sqliteProcess.stderr.toString())
  if(sqliteProcess.status != 0) {
    console.error(
      'Error executing update of injection',
      updateSql,
      sqliteProcess.stderr.toString(),
    )
    process.exit(1)
  }
})
console.log('success')
