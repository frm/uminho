class ReviewsController < ApplicationController
  before_action :authenticate_user!

  def create
    @review = current_user.reviews.create(
                review_params.merge(movie_id: params[:movie_id]))
    redirect_to movie_url(params[:movie_id])
  end

  def destroy
    Review.find(params[:id]).destroy
    redirect_to movie_url(params[:movie_id])
  end

  def update
    @review = Review.find(params[:id])
    @review.update_attributes review_params
    redirect_to movie_url(params[:movie_id])
  end

  private

  def review_params
    params.require(:review).permit(:score, :description)
  end
end
