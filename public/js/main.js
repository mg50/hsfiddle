$(document).ready(function() {
  var codeEditor = ace.edit('code-area').getSession()
  codeEditor.setMode('ace/mode/haskell')

  var htmlEditor = ace.edit('html-area').getSession()
  htmlEditor.setMode('ace/mode/html')

  var cssEditor = ace.edit('css-area').getSession()
  cssEditor.setMode('ace/mode/css')

  var fiddle = $('#fiddle')

  submit = $('#submit-fiddle')
  submit.click(function() {
    $.ajax({
      url: '/compile',
      type: 'post',
      data: {code: codeEditor.getValue()},
      dataType: 'json',
      success: function(r) {
        fillFiddle(htmlEditor.getValue(), cssEditor.getValue(), r.js)
      }
    })
  })

  function fillFiddle(html, css, js) {
    cssTag = '<style type="text/css">' + css + '</style>'
    scriptTag = '<script type="text/javascript">' + js + '</script>'
    iframeHtml = '<html><head>' + cssTag + '</head><body>' + html +
      scriptTag + '</body></html>'
    fiddle.get(0).contentWindow.document.write(iframeHtml);
  }
})
