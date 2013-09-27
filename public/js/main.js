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
    jqueryTag = '<script type="text/javascript" src="./js/jquery.min.js"></script>'
    scriptTag = '<script type="text/javascript">' + js + '</script>'
    iframeHtml = '<html><head>' + jqueryTag + cssTag + '</head><body>' + html +
      scriptTag + '</body></html>'
    fiddleArea.html('<iframe></iframe>')
    fiddleArea.find('iframe').get(0).contentWindow.document.write(iframeHtml);
  }

  var exampleSelector = $('#examples')
  exampleSelector.change(function() {
    var val = $(this).val()
    var example = $('#' + val)
    if(example.length > 0) {
      var exampleCode = example.find('[type="text/ghcjs"]').html()
      var exampleHtml = example.find('[type="text/html"]').html()
      var exampleCss = example.find('[type="text/css"]').html()

      codeEditor.setValue(exampleCode);
      htmlEditor.setValue(exampleHtml);
      cssEditor.setValue(exampleCss);
    }
  })
})
