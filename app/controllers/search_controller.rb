class SearchController < ApplicationController
  def index
    @results = TMDB::Searcher.search(params[:q])
    users = user_search(params[:q])
    @results[:users] = users unless users.empty?
    @results
  end

  def actor
    @actors = TMDB::Searcher.actor_search(params[:q])
    @genre = params[:genre_id]
    respond_to do |format|
      format.html { redirect_to genres_path }
      format.js
    end
  end

  def user
    @users = user_search(params[:q])
    @genre = params[:genre_id]
    respond_to do |format|
      format.html { redirect_to genres_path }
      format.js
    end
  end

  private

  def user_search(query)
    User.where("name LIKE ? OR email LIKE ?",
      "%#{query}%", "%#{query}%")
  end
end
