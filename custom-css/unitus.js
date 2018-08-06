(function() {
  setTimeout(function() {
    var answers = Array.from(document.querySelectorAll('[name="Answer"]'))
    var validAnswers = process.unitusAnswers
    var answer = answers.filter(function(n) {
      return validAnswers.includes(n.value)
    })[0]
    if(answer) {
      console.log('selecting', answer)
      answer.checked = true

      var submitButton = document.querySelector('#btn_sauth_sc_submit')
      submitButton.click()
    }
    else {
      console.log('no security questions found')
    }
  }, 500)
})()
