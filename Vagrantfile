# -*- mode: ruby -*-
# vi: set ft=ruby :

# All Vagrant configuration is done below. The "2" in Vagrant.configure
# configures the configuration version (we support older styles for
# backwards compatibility). Please don't change it unless you know what
# you're doing.
Vagrant.configure("2") do |config|
  # The most common configuration options are documented and commented below.
  # For a complete reference, please see the online documentation at
  # https://docs.vagrantup.com.

  # Every Vagrant development environment requires a box. You can search for
  # boxes at https://atlas.hashicorp.com/search.
  config.vm.box = "ubuntu/xenial64"

  # Disable automatic box update checking. If you disable this, then
  # boxes will only be checked for updates when the user runs
  # `vagrant box outdated`. This is not recommended.
  # config.vm.box_check_update = false

  # Create a forwarded port mapping which allows access to a specific port
  # within the machine from a port on the host machine. In the example below,
  # accessing "localhost:8080" will access port 80 on the guest machine.
  # config.vm.network "forwarded_port", guest: 80, host: 8080

  # Create a private network, which allows host-only access to the machine
  # using a specific IP.
  # config.vm.network "private_network", ip: "192.168.33.10"

  # Create a public network, which generally matched to bridged network.
  # Bridged networks make the machine appear as another physical device on
  # your network.
  # config.vm.network "public_network"

  # Share an additional folder to the guest VM. The first argument is
  # the path on the host to the actual folder. The second argument is
  # the path on the guest to mount the folder. And the optional third
  # argument is a set of non-required options.
  # config.vm.synced_folder "../../Guix/guix-clean", "/vagrant_guix-clean"

  # Provider-specific configuration so you can fine-tune various
  # backing providers for Vagrant. These expose provider-specific options.
  # Example for VirtualBox:
  #
  # config.vm.provider "virtualbox" do |vb|
  #   # Display the VirtualBox GUI when booting the machine
  #   vb.gui = true
  #
  #   # Customize the amount of memory on the VM:
  #   vb.memory = "1024"
  # end
  config.vm.provider "virtualbox" do |vb|
    vb.memory = "2048"
    vb.cpus = "2"
  end
  #
  # View the documentation for the provider you are using for more
  # information on available options.

  # Define a Vagrant Push strategy for pushing to Atlas. Other push strategies
  # such as FTP and Heroku are also available. See the documentation at
  # https://docs.vagrantup.com/v2/push/atlas.html for more information.
  # config.push.define "atlas" do |push|
  #   push.app = "YOUR_ATLAS_USERNAME/YOUR_APPLICATION_NAME"
  # end

  config.vm.provision "shell", inline: <<-SHELL
    set -exu

    export DEBIAN_FRONTEND=noninteractive
    apt-get update
    apt-get install -y language-pack-en htop

    cd /tmp

    wget -nc -nv ftp://alpha.gnu.org/gnu/guix/guix-binary-0.11.0.x86_64-linux.tar.xz
    wget -nc -nv ftp://alpha.gnu.org/gnu/guix/guix-binary-0.11.0.x86_64-linux.tar.xz.sig

    KEY_ID="090b11993d9aebb5"
    gpg --batch --keyserver pgp.mit.edu --recv-keys $KEY_ID
    gpg --batch --trusted-key $KEY_ID \
                --verify guix-binary-0.11.0.x86_64-linux.tar.xz.sig \
                         guix-binary-0.11.0.x86_64-linux.tar.xz

    tar --warning=no-timestamp -xf guix-binary-0.11.0.x86_64-linux.tar.xz
    rm -rf /var/guix
    mv var/guix /var/
    rm -rf /gnu
    mv gnu /

    ln -sf /var/guix/profiles/per-user/root/guix-profile /root/.guix-profile

    groupadd --system guixbuild
    for i in `seq -w 1 10`;
    do
      useradd -g guixbuild -G guixbuild           \
              -d /var/empty -s `which nologin`    \
              -c "Guix build user $i" --system    \
              guixbuilder$i;
    done

    cp /root/.guix-profile/lib/systemd/system/guix-daemon.service \
       /etc/systemd/system/
    systemctl start guix-daemon && systemctl enable guix-daemon

    mkdir -p /usr/local/bin
    ln -sf /var/guix/profiles/per-user/root/guix-profile/bin/guix /usr/local/bin/guix

    guix archive --authorize < /root/.guix-profile/share/guix/hydra.gnu.org.pub

    cat <<- EOF > /home/ubuntu/.guile
    (use-modules (ice-9 readline))
    (activate-readline)
    EOF
  SHELL
end
