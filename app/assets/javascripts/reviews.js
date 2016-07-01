$(function() {
  var rating_params = {
    step: 0.5,
    stars: 5,
    size: 'xs',
    max: 5
  };

  $('#review_score').rating(rating_params);
  $('.review-show-score').rating(rating_params);
});
