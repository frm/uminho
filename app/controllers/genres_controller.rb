class GenresController < ApplicationController
  before_action :authenticate_user!

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
        @movies << m if m.genre_ids.include? @genre.id
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
        @movies << m if m.genre_ids.include? @genre.id
      end
    end

    render :show
  end

  def suggest
    @genres = Genre.all

    if current_user.reviews.count == 0
      # No reviews, no learning. Randomize
      @movie = Movie.trending.sample
    elsif current_user.following.count == 0
      # No following, no tailoring.
      # Sample movie from a favorite genre.
      @movie = Genre.movies(current_user.favorite_genres.sample).sample
    else
      @movie = tailored_movie
    end
  end

  private

  def movie_sort(movies)
    overall_rating = {}

    movies.each do |m|
      score = overall_rating[m.id] || 0
      overall_rating[m.id] = score + m.rating
    end

    overall_rating.sort_by { |movie, score| score }
  end

  def tailored_movie
    reliable_users = current_user.reliable_following(3)

    top_movies = reliable_users.flat_map { |u| current_user.tailored_from u }

    if top_movies.empty?
      Genre.movies(current_user.favorite_genres.sample).sample
    else
      Movie.find(movie_sort(top_movies).last(10).map(&:first).sample)
    end
  end
end
