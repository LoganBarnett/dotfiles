// The CSS overriding has a problem in Firefox. When onLoad is false, the
// scripts are injected immediately. However this injection doesn't seem to work
// for navigation events which means we have to do a hard refresh on any
// navigation. When onLoad is true, there is a white flash that happens due to
// the injections waiting for the page assets to finish loading. To remedy this
// we have a preload.css file that will be applied to everything. When we apply
// this to absolutely everything we wind up breaking sites that have not been
// dark-themed yet. To remedy that problem, we construct its "includes" to be
// all of the existing JSON files we have.

const fs = require('fs')
const path = require('path')

module.exports = () => {
  const files = fs.readdirSync('custom-css')
  const jsonFiles = files
    .filter(f => f.match(/\.json$/))
    .filter(f => !f.match(/preload\.json$/))
  const jsons = jsonFiles.map(f => fs.readFileSync(
    path.join('custom-css', f),
    'utf8',
  ))
  return jsons.reduce((acc, json) => acc.concat(json.includes), [])
}
