class MoviesController < ApplicationController
  def show
    @movie = Movie::TraktLoader.find(params[:id])
  end

  def index
    @movies = Movie::TraktLoader.all
  end
end
