#! /usr/bin/env node

/**
 * This script is expected to be called by read-safari-usercss.sh as it iterates
 * over all of the keys.
 *
 * $1: Path to the JSON file that links to the other files.
 * $2: Directory to write resulting output to.
 * $3: Key of the JSON file (used for file names).
 */

// TODO: Install the extension for this dynamically. Links of relevance:
// https://github.com/tsbehlman/Injector/issues/1
// http://tbehlman.com/safari/Injector.safariextz
const process = require('process')
const fs = require('fs')
const path = require('path')

const fileName = process.argv[2]
const outputDir = process.argv[3]
const key = process.argv[4]

const file = fs.readFileSync(fileName, 'utf8')
const customCssObj = JSON.parse(file)

const newFileName = customCssObj.name
const css = customCssObj.styles
// console.log('keys', Object.keys(customCssObj))
// console.log(key, 'script', customCssObj.script)

const baseFileName = path.resolve(outputDir, newFileName)
fs.writeFileSync(baseFileName + '.css', css)
fs.writeFileSync(baseFileName + '.js', customCssObj.script)

// also store the meta data
delete customCssObj.styles
delete customCssObj.script
customCssObj.key = key
fs.writeFileSync(baseFileName + '.json', JSON.stringify(customCssObj, null, 2))

// clean up when done
fs.unlinkSync(fileName)
