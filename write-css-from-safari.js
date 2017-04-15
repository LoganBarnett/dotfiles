#! /usr/bin/env node

const process = require('process')
const fs = require('fs')
const path = require('path')

const fileName = process.argv[2]
const outputDir = process.argv[3]

const file = fs.readFileSync(fileName, 'utf8')
const customCssObj = JSON.parse(file)

const newFileName = customCssObj.name
const css = customCssObj.styles

const baseFileName = path.resolve(outputDir, newFileName)
fs.writeFileSync(baseFileName + '.css', css)

// also store the meta data
delete customCssObj.styles
fs.writeFileSync(baseFileName + '.json', JSON.stringify(customCssObj, null, 2))

// clean up when done
fs.unlink(fileName)
