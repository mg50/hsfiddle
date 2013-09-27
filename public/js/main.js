$(document).ready(function() {
  ace.edit('code-area').getSession().setMode('ace/mode/haskell')
  ace.edit('html-area').getSession().setMode('ace/mode/html')
  ace.edit('css-area').getSession().setMode('ace/mode/css')


  htmlArea = $('#html-area')
  codeArea = $('#code-area')
  cssArea = $('#css-area')
  fiddle = $('#fiddle')

  submit = $('#submit-fiddle')
  submit.click(function() {
    $.ajax({
      url: '/compile',
      data: {code: encodeURIComponent(fiddle.html())},
      dataType: 'json',
      success: function(r) {
        fillFiddle(htmlArea.html(), cssArea.html(), r.js)
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
