'use strict'

const addButtonRole = () => {
  console.log('Got change event...')
  const buttonCandidates = document.querySelectorAll('.grouptab.smooth-fade')
  console.log('Count of potentially not buttons: ', buttonCandidates.length)
  buttonCandidates.forEach(e => {
    if(e.getAttribute('role') != 'button') {
      console.log('Making a button-like thing into a button...')
      e.setAttribute('role', 'button')
    }
  })
}

const waitForRail = () => {
  console.log('Waiting for rail...')
  const observerConf = { attributes: true, childList: true, subtree: true }
  const bodyObserver = new MutationObserver((_, observer) => {
    const rail = document.querySelector('.leftrail')
    console.log('Mutation fired. Rail?', rail)
    if(rail != null) {
      console.log('Found a rail! Hooking onto it...')
      hookUpRail()
      bodyObserver.disconnect()
    }
  })
  bodyObserver.observe(document.documentElement, observerConf)
}

const hookUpRail = () => {
  const rail = document.querySelector('.leftrail')
  const observerConf = { attributes: true, childList: true, subtree: true }
  const railObserver = new MutationObserver(addButtonRole)
  railObserver.observe(rail, observerConf)
}

console.log('Running glip button fix.')
waitForRail()

const asshats = [
  '937746120707',
]

const waitForChatEntry = () => {
  console.log('Waiting for chat entry...')
  const observerConf = { attributes: true, childList: true, subtree: true }
  const bodyObserver = new MutationObserver((_, observer) => {
    document.querySelectorAll('.post')
  })
}
// Might be better depending on your injection mechanism.
//
// window.addEventListener('load', () => {
//   console.log('loaded')
//   waitForRail()
// })

// Doesn't work yet. Keys do not appear but all events appear to be fired.
/*
document.querySelectorAll('.chat-textarea').forEach(n => {
  console.log('added event listener')
  n.addEventListener('keyup', e => {
  if(e.key == ' ') {
    console.info('sending key ')
    const cancelled = e.target.dispatchEvent(new KeyboardEvent('keyup', {key: 'a', cancelable : false}))
    console.info('canceled?', cancelled)
  }
}
)})
*/
