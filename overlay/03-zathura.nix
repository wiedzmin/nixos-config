self: super:

{
    zathura = super.zathura.override {
        synctexSupport = false;
    };
}
