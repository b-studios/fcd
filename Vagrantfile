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

    # Refresh sources
    sudo apt-get update -y

    # Graphviz for printing parsers to graphs
    sudo apt-get install -y graphviz

    # Java
    sudo apt-get install -y openjdk-7-jdk

    # Sbt
    sudo mkdir -p /home/vagrant/bin
    pushd /home/vagrant/bin/
    sudo wget https://repo.typesafe.com/typesafe/ivy-releases/org.scala-sbt/sbt-launch/0.13.8/sbt-launch.jar
    sudo cp /home/vagrant/configs/sbt.sh /home/vagrant/bin/sbt
    sudo chmod u+x /home/vagrant/bin/sbt
    sudo chmod +x /home/vagrant/bin/sbt
    popd
  SHELL

end
