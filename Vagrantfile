# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure(2) do |config|

  config.vm.box = "ubuntu/trusty64"

  config.vm.synced_folder "./artifact", "/home/vagrant/artifact"
  config.vm.synced_folder "./configs", "/home/vagrant/configs"

  config.vm.provider "virtualbox" do |v|
    v.memory = 2048
    v.cpus = 4
  end

  config.vm.provision "shell", inline: <<-SHELL

    # Refresh system
    sudo apt-get update -y
    sudo apt-get upgrade -y

    # Graphviz for printing parsers to graphs
    sudo apt-get install -y graphviz

    # Sbt
    sudo apt-get install -y apt-transport-https curl gnupg
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | sudo tee /etc/apt/sources.list.d/sbt.list
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | sudo tee /etc/apt/sources.list.d/sbt_old.list
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | sudo -H gpg --no-default-keyring --keyring gnupg-ring:/etc/apt/trusted.gpg.d/scalasbt-release.gpg --import
    sudo chmod 644 /etc/apt/trusted.gpg.d/scalasbt-release.gpg
    sudo apt-get update -y
    sudo apt-get install -y sbt
    popd
  SHELL

end
