$(function() {
  var quotes = [
  {
    beginning: "It's funny how the colors of the real world",
    punchline: 'only seem really real when you watch them on a screen',
    author: 'Anthony Burgess'
  }];

  var quote = quotes[Math.floor(Math.random() * quotes.length)];

  var landingQuote = $('.landing-quote');
  landingQuote.append(quote.beginning + ' ');
  landingQuote.append("<span>" + quote.punchline + "</span>");

  // var words = quote.punchline.split(' ');

  // words.forEach(function(w, i) {
  //   landingQuote.append("<span>" + w + " </span>");
  // });

  landingQuote.append("<span class='landing-quote-author'>"
      + " - " + quote.author + "</span>");
});
