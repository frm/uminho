class CreateMovies < ActiveRecord::Migration
  def change
    create_table :movies do |t|
      t.string :title
      t.integer :year
      t.string :img_path
      t.text :overview
      t.text :tagline
      t.integer :runtime
      t.string :language

      t.timestamps null: false
    end
  end
end
