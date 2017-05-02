#! /usr/bin/env node

// NOTE: If the db is ever corrupted by a bad set of 'value', the first record
// is always key = 'StyleStorageVersion', and the value = '1.7.0'. The value is
// also in the same utf16le encoding that the rest of it is.
const fs = require('fs')

const readFileData = (path) => {
  try {
    return fs.readFileSync(path, 'utf8')
  }
  catch(e) {
    return ''
  }
}

const process = require('process')
const path = require('path')
const childProcess = require('child_process')
const os = require('os')
const Iconv = require('iconv').Iconv

const names = fs.readdirSync('custom-css')
  .filter(f => f.match(/.+\.json$/))
  .map(f => f.replace(/\.json$/, ''))

names.forEach(name => {
  const meta = JSON.parse(fs.readFileSync(
    path.join('custom-css', name + '.json'),
    'utf8'
  ))

  meta.key = meta.key || name

// const keySql = `select key from ItemTable;`
// // TODO: error handling
// const getKeyProc = childProcess.spawnSync('sqlite3', [sqlitePath, keySql])
// const keys = getKeyProc.stdout.toString().split('\n')

  const css = readFileData(`custom-css/${name}.css`, 'utf8')
  let js = readFileData(`custom-css/${name}.js`, 'utf8')
  if(js == 'undefined') js = ''

// const metaText = fs.readFileSync(`custom-css/${name}.json`, 'utf8')
// const meta = JSON.parse(metaText)
  meta.styles = css
  meta.script = js

  console.log('sending meta', meta)
  const convertedMeta = new Iconv('UTF-8', 'UTF-16LE')
      .convert(JSON.stringify(meta))

  fs.writeFileSync(`.tmp-css-write/${name}.json`, convertedMeta, 'binary')


  const localStorageDir = '~/Library/Safari/LocalStorage'
    .replace('~', os.homedir())

  const files = fs.readdirSync(localStorageDir)
  const findSqliteFile = /safari-extension_com\.tsbehlman\.injector-(.+?)\.localstorage/

  const sqlitePath = path.join(
    localStorageDir,
    files.find(f => f.match(findSqliteFile))
  )

    const updateSql = `
pragma encoding = "UTF-8";
delete from itemtable where key='${meta.key}';
insert into itemtable
(key, value) values
('${meta.key}', readfile('.tmp-css-write/${meta.name}.json'))
;`

    console.log(updateSql)
    const sqliteProcess = childProcess.spawn('sqlite3', [
      sqlitePath,
      updateSql,
    ])
})
// fs.unlinkSync(sqlitePath + '-wal')
console.log('success')
