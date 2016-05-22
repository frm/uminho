class GenresController < ApplicationController
  def show
    @genre = Genre.all.select{ |g| g.id == params[:id].to_i }.first
  end

  def index
    @genres = Genre.all
  end
end
