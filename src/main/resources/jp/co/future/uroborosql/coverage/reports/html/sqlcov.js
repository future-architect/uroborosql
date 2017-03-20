hljs.initHighlightingOnLoad();

$(function() {
    $(".hljs-comment").each(function() {
       if (!$(this).text().match(/\/\*(IF.*)|(ELSE)|(END)|(BEGIN)\*\//) &&
           $(this).text().match(/\/\*[\w_]+\*\//)) {
           $(this).wrapInner('<span class="hljs-param"></span>');
       }
    });
});

