// TODO: I should move relevant bits of ./write-safari-css.js to here since it
// can share code with bootstrap-injector.js.
const fs               = require('fs')
const path             = require('path')
const postcss          = require('postcss')
const customProperties = require('postcss-custom-properties')
const cssVars          = require('./css-vars.js')

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
  const vars = require('../dotfiles-private/js-vars.private.js')
  let result = js
  for(let key in vars) {
    result = result.replace(`process.${key}`, vars[key])
  }
  return result
}

const findInjectorsForDir = (dir) => {
  const names = fs.readdirSync(dir)
        .filter(f => f.match(/.+\.json$/))
        .map(f => f.replace(/\.json$/, ''))


  const injectorData = names.map(name => {
    const meta = JSON.parse(fs.readFileSync(
      path.join(dir, name + '.json'),
      'utf8'
    ))

    meta.key = meta.key || name

    const css = readFileData(`${dir}/${name}.css`)
    let js = readFileData(`${dir}/${name}.js`)
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
    return meta
  })

  return injectorData
}

module.exports = () => {
  return findInjectorsForDir('custom-css').concat(
    findInjectorsForDir('../dotfiles-private/custom-css'),
  )
}
