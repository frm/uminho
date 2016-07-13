class ActorsDecorator < Draper::CollectionDecorator
  def list(connector)
    object.map(&:name).to_sentence(last_word_connector: " #{connector} ")
  end
end
