#! /usr/bin/env node

const process = require('process')
const fs = require('fs')
const path = require('path')
const childProcess = require('child_process')
const os = require('os')
const Iconv = require('iconv').Iconv



// sqlitePath

const css = fs.readFileSync('custom-css/github.css', 'utf8')

const metaText = fs.readFileSync('custom-css/github.json', 'utf8')
const meta = JSON.parse(metaText)
meta.styles = css

const convertedMeta = new Iconv('UTF-8', 'UTF-16LE').convert(JSON.stringify(meta))
// const convertedMeta = JSON.stringify(meta).replace(/(.)/g, '$1\0')

console.log('converted meta', convertedMeta.toString())
fs.writeFileSync('./.tmp-css-write/github.json', convertedMeta, 'binary')

// const sqliteProcess = require('child_process').spawn('sqlite3', [sqlitePath])
// sqliteProcess.stdin.write() 

const keySql = `select key from ItemTable;`

const localStorageDir = '~/Library/Safari/LocalStorage'.replace('~', os.homedir())
const files = fs.readdirSync(localStorageDir)
const findSqliteFile = /safari-extension_com\.tsbehlman\.injector-(.+?)\.localstorage/

const sqlitePath = path.join(
  localStorageDir,
  files.find(f => f.match(findSqliteFile))
)

console.log('path', sqlitePath)

const getKeyProc = childProcess.spawnSync('sqlite3', [sqlitePath, keySql])
// TODO: don't assume first - get multiple.
console.log('output', getKeyProc.stdout.toString(), getKeyProc.stderr.toString())
const key = getKeyProc.stdout.toString().split('\n')[0]
const updateSql = `
pragma encoding = "UTF-8";
update itemtable
set value=readfile('./.tmp-css-write/github.json')
where key='${key}'
;`

console.log(updateSql)
const sqliteProcess = require('child_process').spawn('sqlite3', [
  sqlitePath,
  updateSql,
])

// fs.unlinkSync(sqlitePath + '-wal')
console.log('success')
