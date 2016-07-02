module MoviesHelper
  def movie_badge_color(movie)
    case movie.rating
    when 0..2.0
      "red"
    when 2.5..3.5
      "blue"
    when 4..5
      "green"
    end
  end
end
