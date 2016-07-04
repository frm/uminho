Rails.application.routes.draw do
  # devise_for :users
  root 'welcome#index'

  #resources :users, only: [:index, :show] do
  #  get :following, :followers
  #end

  #get '/movies/trending' => 'movies#trending', as: 'trending_movies'
  #get '/movies/releases' => 'movies#releases', as: 'movie_releases'
  #get '/movies/popular'  => 'movies#popular', as: 'popular_movies'
  #get '/movies/upcoming' => 'movies#upcoming', as: 'upcoming_movies'

  #resources :movies, only: [:show] do
  #  resources :reviews, only: [:create, :destroy, :update]
  #end

  #resources :actors, only: [:show]

  #resources :genres, only: [:index, :show] do
  #  get '/search/actor' => 'genres#actor', as: 'actor_search'
  #  get '/search/user' => 'genres#user', as: 'user_search'
  #end

  #get '/suggest' => 'genres#suggest', as: 'suggestion'

  #resources :relationships, only: [:create, :destroy]

  #get '/search' => 'search#index', as: 'search'
  #get '/search/actor' => 'search#actor', as: 'actor_search'
  #get '/search/user' => 'search#user', as: 'user_search'

  #scope '/reviews' do
  #  post '/:review_id/vote' => 'ratings#create', as: 'reviews_vote'
  #  delete '/:review_id/unvote' => 'ratings#destroy', as: 'reviews_unvote'
  #end

  namespace :api do
    namespace :v1 do
      resources :genres, only: [:show]
    end
  end
end
