// This takes the custom injections (css and js) I have and writes them to
// bootstrap.js inside of the Injector.safariextension repository. The contents
// of the file implicitly is written to the localstorage at startup time. I've
// added a script tag that imports the file before storage.js runs.

// This script is mostly complete. Last time I used it, I needed to save an
// injection to get injections actually working, even though there were no
// changes. bootstrap.js might need to get imported last and then run some
// function that applies the injections.
const fs = require('fs')
const loader = require('./load-injector-data.js')

const lines = loader().map(m => {
  return `\
localStorage.setItem( "${m.key}", JSON.stringify(${JSON.stringify(m)}))\
`
})


fs.writeFileSync(
  '/Users/logan/dev/Injector.safariextension/share/bootstrap.js',
  lines.join('\n') + `
styleStorage.update()
reloadStyles()
`
)
