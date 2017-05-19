#! /usr/bin/env n use 7.7.4

// NOTE: If the db is ever corrupted by a bad set of 'value', the first record
// is always key = 'StyleStorageVersion', and the value = '1.7.0'. The value is
// also in the same utf16le encoding that the rest of it is.
const fs = require('fs')
const postcss = require('postcss')
const customProperties = require('postcss-custom-properties')
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
  meta.script = js

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

  // console.log(updateSql)
  const sqliteProcess = childProcess.spawnSync('sqlite3', [
    sqlitePath,
    updateSql,
  ])

  if(sqliteProcess.status != 0) {
    console.error(
      'Error executing update of injection',
      updateSql,
      sqliteProcess.stderr.toString()
    )
  }
})
console.log('success')
