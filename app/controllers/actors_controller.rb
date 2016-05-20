class ActorsController < ApplicationController
  def show
    @actor = Actor.find(params[:id])
  end
end
