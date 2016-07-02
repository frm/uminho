class ReviewDecorator < Draper::Decorator
  delegate_all

  def color
    if object.reliability == 0
      "grey"
    elsif object.reliability > 0
      "green"
    else
      "red"
    end
  end
end
