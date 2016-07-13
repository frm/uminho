class ActorDecorator < Draper::Decorator
  delegate_all

  def birth_info
    "#{actor.age} years old, #{actor.place_of_birth}"
  end

  def list(connector)
    object.map(&:name).to_sentence(last_word_connector: " #{connector} ")
  end
end
