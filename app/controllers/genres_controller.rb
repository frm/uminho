class GenresController < ApplicationController
  layout "sidebar"

  def show
    @genres = Genre.all
    @genre = @genres.select{ |g| g.id == params[:id].to_i }.first
    @movies = @genre.movies
  end

  def index
    @genres = Genre.all
    @movies = Movie.trending
  end

  def actor
    @movies = []
    @actors = []
    @genres = Genre.all
    @genre = @genres.select{ |g| g.id == params[:genre_id].to_i }.first

    params[:actors].each do |a|
      actor = Actor.find(a)
      @actors << actor
      actor.movies.each do |m|
        @movies << m if m.genres.include? @genre.id
      end
    end

    render :show
  end

  def user
    @movies = []
    @users = []
    @genres = Genre.all
    @genre = @genres.select{ |g| g.id == params[:genre_id].to_i }.first

    params[:users].each do |u|
      user = User.find u
      @users << user
      user.reviewed_movies.each do |m|
        @movies << m if m.genres.include? @genre.id
      end
    end

    render :show
  end
end
