class MovieDecorator < Draper::Decorator
  delegate_all

  def badge_color
    case object.rating
    when 0..2.0
      "red"
    when 2.5..3.5
      "blue"
    when 4..5
      "green"
    end
  end

  def directors_sentence
    object.directors.map(&:name).to_sentence(words_connector: ', ',
                                             last_word_connector: ' and ')
  end

  def genres_sentence
    object.genre_names.to_sentence(words_connector: ', ',
                                             last_word_connector: ' and ')
  end
end
