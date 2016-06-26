$(function() {
  var quotes = [
  {
    beginning: "It's funny how the colors of the real world",
    punchline: 'only seem really real when you watch them on a screen',
    author: 'Anthony Burgess'
  },
  {
    beginning: "Cinema is a matter of what's in the frame",
    punchline: "and what's out.",
    author: 'Martin Scorsese'
  },
  {
    beginning: "I have a theory that movies operate",
    punchline: "on the level of dreams where you dream yourself",
    author: "Meryl Streep"
  },
  {
    beginning: "Art reflects society. Cinema doesn't dictate - ",
    punchline: "you portray what the society demands",
    author: "Sonam Kapoor"
  },
  {
    beginning: "Dialogue should simply be a sound among other sounds, just something that comes",
    punchline: "out of the mouths of people whose eyes tell the story in visual terms",
    author: "Alfred Hitchcock"
  },
  {
    beginning: "There are no rules in filmmaking. Only sins.",
    punchline: "And the cardinal sin is dullness",
    author: "Frank Capra"
  },
  {
    beginning: "There's nothing creative",
    punchline: "about living within your means",
    author: "Francis Ford Coppola"
  }
  ];

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
