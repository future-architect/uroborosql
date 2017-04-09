hljs.initHighlightingOnLoad();

$(function () {
    $(".hljs-comment").each(function () {
        if (!$(this).text().match(/\/\*(IF.*)|(ELIF.*)|(ELSE)|(END)|(BEGIN)\*\//) &&
            $(this).text().match(/\/\*[\w_]+\*\//)) {
            $(this).wrapInner('<span class="hljs-param"></span>');
        }
        var html = $(this).html();
        $(this).html(
            html.replace(/(\/\*)(IF\s|ELIF\s|ELSE|END|BEGIN)/,
                '$1<span class="hljs-comment-statement">$2</span>'));
    });
});

