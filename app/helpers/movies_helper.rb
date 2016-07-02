module MoviesHelper
  def rating_badge_color(rating)
    case rating
    when 0..2.0
      "red"
    when 2.5..3.5
      "blue"
    when 4..5
      "green"
    end
  end
end
