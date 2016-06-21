class WelcomeController < ApplicationController
  def index
    @activities = PublicActivity::Activity.all if current_user
  end
end
