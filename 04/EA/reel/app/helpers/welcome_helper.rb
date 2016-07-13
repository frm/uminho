module WelcomeHelper
  def random_cinemagraph
    cinemagraph = cinemagraphs.sample
    asset_path cinemagraph_path(cinemagraph)
  end

  private

  def cinemagraphs
    blacklist = [".", ".."]
    cinemagraphs = Dir.glob("app/assets/images/cinemagraphs/*")
    blacklist.each do |blacklisted|
      cinemagraphs.delete(blacklisted)
    end

    cinemagraphs
  end

  def cinemagraph_path(cinemagraph)
    cinemagraph.split(File::SEPARATOR)[-2..-1].join(File::SEPARATOR)
  end
end
