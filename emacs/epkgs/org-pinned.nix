{ callPackage }: {
    org = callPackage ({ elpaBuild, fetchurl, lib }: elpaBuild {
        pname = "org";
        version = "20180103";
        src = fetchurl {
          url = "https://orgmode.org/elpa/org-20180103.tar";
          sha256 = "1hyw9sigcv9wn37y2icmhf1czf0s3dgvsmn36355l95zsw7hnvgj";
        };
        packageRequires = [];
        meta = {
          homepage = "https://elpa.gnu.org/packages/org.html";
          license = lib.licenses.free;
        };
      }) {};
    org-plus-contrib = callPackage ({ elpaBuild, fetchurl, lib }: elpaBuild {
        pname = "org-plus-contrib";
        version = "20180103";
        src = fetchurl {
          url = "https://orgmode.org/elpa/org-plus-contrib-20180103.tar";
          sha256 = "164i2asqh34p1g3iqsn7rziyxbi1ys8fwdmn7nsw5xph8qszv9zj";
        };
        packageRequires = [];
        meta = {
          homepage = "https://elpa.gnu.org/packages/org-plus-contrib.html";
          license = lib.licenses.free;
        };
      }) {};
  }
