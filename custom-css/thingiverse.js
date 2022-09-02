// Add a download-all-files button.

window.onload = () => {
  const el = document.createElement('button')
  el.onclick = (e) => {
    e.preventDefault()
    window.open(window.location.href + '/zip', '_blank')
  }
  el.innerText = 'download all files'
  document.querySelector('#page-header').appendChild(el)
}
