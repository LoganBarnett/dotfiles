function selectQuestionPath() {
  const questionInput = document.querySelector('#sa_scm_dcrd_questionInput')
  if(questionInput) {
    console.log('Choosing "security" questions...')
    questionInput.checked = true
    const submitButton = document.querySelector('#btn_sauth_cc_submit')
    if(submitButton) {
      submitButton.click()
    } else {
      console.error('Couldn\'t find submit button!')
    }
  } else {
    console.log('No security question prompt found.')
  }
}

// You're being passive aggressive again...
function answerQuoteSecurityQuestionUnquote() {
  var answers = Array.from(document.querySelectorAll('[name="Answer"]'))
  var validAnswers = process.unitusAnswers
  var answer = answers.filter(function(n) {
    return validAnswers.includes(n.value)
  })[0]
  if(answer) {
    console.log('Selecting answer', answer)
    answer.checked = true

    var submitButton = document.querySelector('#btn_sauth_sc_submit')
    submitButton.click()
  }
  else {
    console.log('No security questions found.')
  }
}

(function() {
  setTimeout(function() {
    selectQuestionPath()
    answerQuoteSecurityQuestionUnquote()
  }, 500)
})()
