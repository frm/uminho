$(document).ready(function() {
    $('.accordion :header').on('click', function(e) {
        e.stopPropagation();
        $(this).parent().find('.large-12').slideToggle();
    });
});

