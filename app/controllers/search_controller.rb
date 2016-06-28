class SearchController < ApplicationController
  def index
    @results = TMDB::Searcher.search(params[:q])
    users = User.where("name LIKE ? OR email LIKE ?",
      "%#{params[:q]}%", "%#{params[:q]}%")

    @results[:users] = users unless users.empty?
    @results
  end
end
