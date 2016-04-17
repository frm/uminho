Rails.application.routes.draw do
  devise_for :users
  root 'welcome#index'

  resources :users, only: [:index, :show]

  resources :movies, only: [:index, :show]
end
