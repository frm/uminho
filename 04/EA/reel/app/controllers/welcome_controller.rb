class WelcomeController < ApplicationController
  def index
    @activities = current_user.feed if current_user
    #  render :placeholder
  end
end
