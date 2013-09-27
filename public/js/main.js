$(document).ready(function() {
  var codeEditor = ace.edit('code-area').getSession()
  codeEditor.setMode('ace/mode/haskell')

  var htmlEditor = ace.edit('html-area').getSession()
  htmlEditor.setMode('ace/mode/html')

  var cssEditor = ace.edit('css-area').getSession()
  cssEditor.setMode('ace/mode/css')

  var fiddleArea = $('#fiddle-area')

  var notification = $('#compiling-notification')

  submit = $('#submit-fiddle')
  submit.click(function() {
    notification.css({display: 'inline-block'})

    $.ajax({
      url: '/compile',
      type: 'post',
      data: {code: codeEditor.getValue()},
      dataType: 'json',
      success: function(r) {
        notification.css({display: 'none'})
        if(r.error) console.log(r.error)
        fillFiddle(htmlEditor.getValue(), cssEditor.getValue(), r.js)
      },
      error: function(r) {
        notification.css({display: 'none'})
      }
    })
  })

  function fillFiddle(html, css, js) {
    cssTag = '<style type="text/css">' + css + '</style>'
    scriptTag = '<script type="text/javascript">' + js + '</script>'
    iframeHtml = '<html><head>' + cssTag + '</head><body>' + html +
      scriptTag + '</body></html>'
    fiddleArea.html('<iframe></iframe>')
    fiddleArea.find('iframe').get(0).contentWindow.document.write(iframeHtml);
  }
})
