$(document).ready(function() {
  var config = require('ace/config')
  config.set('workerPath', '/js/vendor/ace')

  var codeEditor = ace.edit('code-area').getSession()
  codeEditor.setMode('ace/mode/haskell')

  var htmlEditor = ace.edit('html-area').getSession()
  htmlEditor.setMode('ace/mode/html')

  var cssEditor = ace.edit('css-area').getSession()
  cssEditor.setMode('ace/mode/css')

  var fiddleArea = $('#fiddle-area')

  var notification = $('#compiling-notification')

  $('#accordion').css('display', 'block').accordion({
    beforeActivate: function() {
      codeEditor.setValue(codeEditor.getValue())
      htmlEditor.setValue(htmlEditor.getValue())
      cssEditor.setValue(cssEditor.getValue())
    }
  })

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
    var cssTag = '<style type="text/css">' + css + '</style>'
    var jqueryTag = '<script type="text/javascript" src="./js/vendor/jquery.min.js"></script>'

    var libTag = '<script type="text/javascript">' + js.lib + '</script>'
    var rtsTag = '<script type="text/javascript src="./js/rts.js"></script>'
    var lib1Tag = '<script type="text/javascript">' + js.lib1 + '</script>'
    var outTag = '<script type="text/javascript">' + js.out + '</script>'

    var iframeHtml = '<html><head>' + jqueryTag + libTag + rtsTag +
      lib1Tag + outTag + cssTag + '</head><body>' + html + '</body></html>'

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

  var tds = $('#editor-row td')
  tds.click(function() {

  })
})
