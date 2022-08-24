const darkModeEnable = () => {
  const html = document.documentElement
  html.classList.remove('__fb-light-mode')
  html.classList.add('__fb-dark-mode')
}

document.onload = darkModeEnable
window.onload = darkModeEnable

darkModeEnable()
