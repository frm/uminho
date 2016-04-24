$(document).ready(function() {
    $('.accordion').on('click', ':header', function(e) {
        e.stopPropagation();
        $(this).toggleClass('open');
        $(this).closest('.accordion').find("> .large-12").slideToggle();
    });
});

