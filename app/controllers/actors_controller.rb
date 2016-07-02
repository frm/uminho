class ActorsController < ApplicationController
  before_action :authenticate_user!

  def show
    @actor = Actor.find(params[:id])
  end
end
