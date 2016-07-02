$(function() {
  console.log($('.loading-trigger'));
  $('.loading-trigger').click(function() {
    $('#loading').fadeIn();
  });

  var actorForm = $('#actor-search-form');
  var userForm = $('#user-search-form');

  actorForm.hide();
  userForm.hide();

  $('#actor-search').click(function() {
    userForm.slideUp();
    actorForm.slideToggle();
  });

  $('#user-search').click(function() {
    actorForm.slideUp();
    userForm.slideToggle();
  })
});
