class RatingsController < ApplicationController
  def create
    @review = Review.find(params[:review_id])
    @review.vote_by voter: current_user, vote: params[:review_rating]
    respond_to do |format|
      format.html { redirect_to @review.movie }
      format.js
    end
  end

  def destroy
    @review = Review.find(params[:review_id])
    @review.unvote_by current_user
    respond_to do |format|
      format.html { redirect_to @review.movie }
      format.js
    end
  end
end
