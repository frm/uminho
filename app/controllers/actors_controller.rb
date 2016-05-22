class ActorsController < ApplicationController
  def show
    @actor = Actor.find(params[:id])
    @movies = @actor.movies
  end
end
