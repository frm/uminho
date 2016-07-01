module RatingsHelper
  def review_color(review)
    if review.reliability == 0
      "grey"
    elsif review.reliability > 0
      "green"
    else
      "red"
    end
  end
end
