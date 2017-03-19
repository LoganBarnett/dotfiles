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

fs.writeFileSync(path.resolve(outputDir, newFileName + '.css'), css)

fs.unlink(fileName)
