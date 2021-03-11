{
  amazing_print = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1rmsbv2kl5gcsk6vx54fbfa5w6jp68s5mxmkgv8v4492djwvlrbk";
      type = "gem";
    };
    version = "1.3.0";
  };
  apipie-bindings = {
    dependencies = ["json" "oauth" "rest-client"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "11cnpa94064jiv17wzpvjl9v6r4i4y7clqscshkx157z0prn4qgh";
      type = "gem";
    };
    version = "0.4.0";
  };
  clamp = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0l52p47dajjh3562mdiiqsiyam8lkkr4bdfma66jp6jdycy16f7k";
      type = "gem";
    };
    version = "1.1.2";
  };
  domain_name = {
    dependencies = ["unf"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0lcqjsmixjp52bnlgzh4lg9ppsk52x9hpwdjd53k8jnbah2602h0";
      type = "gem";
    };
    version = "0.5.20190701";
  };
  fast_gettext = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0s4c7w2rfqi2n7sjlaqj498c6cdnn5k64bj7lcl4fi22xri21893";
      type = "gem";
    };
    version = "2.0.3";
  };
  hammer_cli = {
    dependencies = ["amazing_print" "apipie-bindings" "clamp" "fast_gettext" "highline" "locale" "logging" "unicode" "unicode-display_width"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0bj35w8hhjwvaxp7ymc5i2vyczcbqz35x1kr40iyfl04li6vm43f";
      type = "gem";
    };
    version = "2.4.0";
  };
  hammer_cli_foreman = {
    dependencies = ["apipie-bindings" "hammer_cli" "jwt" "rest-client"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1lr6ykw9hza5kyn58127i94v58q6ql45951fs64wclchh6fmafsq";
      type = "gem";
    };
    version = "2.4.0";
  };
  hammer_cli_foreman_ssh = {
    dependencies = ["hammer_cli" "hammer_cli_foreman" "net-ssh-multi"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1436xgrmryxf1dhi2l3wwdbd6jvbi5lvvq46l48i32yb6f7yj531";
      type = "gem";
    };
    version = "0.0.3";
  };
  highline = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0yclf57n2j3cw8144ania99h1zinf8q3f5zrhqa754j6gl95rp9d";
      type = "gem";
    };
    version = "2.0.3";
  };
  http-accept = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "09m1facypsdjynfwrcv19xcb1mqg8z6kk31g8r33pfxzh838c9n6";
      type = "gem";
    };
    version = "1.7.0";
  };
  http-cookie = {
    dependencies = ["domain_name"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "004cgs4xg5n6byjs7qld0xhsjq3n6ydfh897myr2mibvh6fjc49g";
      type = "gem";
    };
    version = "1.0.3";
  };
  json = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0lrirj0gw420kw71bjjlqkqhqbrplla61gbv1jzgsz6bv90qr3ci";
      type = "gem";
    };
    version = "2.5.1";
  };
  jwt = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "14ynyq1q483spj20ffl4xayfqx1a8qr761mqjfxczf8lwlap392n";
      type = "gem";
    };
    version = "2.2.2";
  };
  little-plugger = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1frilv82dyxnlg8k1jhrvyd73l6k17mxc5vwxx080r4x1p04gwym";
      type = "gem";
    };
    version = "1.1.4";
  };
  locale = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0997465kxvpxm92fiwc2b16l49mngk7b68g5k35ify0m3q0yxpdn";
      type = "gem";
    };
    version = "2.1.3";
  };
  logging = {
    dependencies = ["little-plugger" "multi_json"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0pkmhcxi8lp74bq5gz9lxrvaiv5w0745kk7s4bw2b1x07qqri0n9";
      type = "gem";
    };
    version = "2.3.0";
  };
  mime-types = {
    dependencies = ["mime-types-data"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1zj12l9qk62anvk9bjvandpa6vy4xslil15wl6wlivyf51z773vh";
      type = "gem";
    };
    version = "3.3.1";
  };
  mime-types-data = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1phcq7z0zpipwd7y4fbqmlaqghv07fjjgrx99mwq3z3n0yvy7fmi";
      type = "gem";
    };
    version = "3.2021.0225";
  };
  multi_json = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0pb1g1y3dsiahavspyzkdy39j4q377009f6ix0bh1ag4nqw43l0z";
      type = "gem";
    };
    version = "1.15.0";
  };
  net-ssh = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0jp3jgcn8cij407xx9ldb5h9c6jv13jc4cf6kk2idclz43ww21c9";
      type = "gem";
    };
    version = "6.1.0";
  };
  net-ssh-gateway = {
    dependencies = ["net-ssh"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1l3v761y32aw0n8lm0c0m42lr4ay8cq6q4sc5yc68b9fwlfvb70x";
      type = "gem";
    };
    version = "2.0.0";
  };
  net-ssh-multi = {
    dependencies = ["net-ssh" "net-ssh-gateway"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "13kxz9b6kgr9mcds44zpavbndxyi6pvyzyda6bhk1kfmb5c10m71";
      type = "gem";
    };
    version = "1.2.1";
  };
  netrc = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0gzfmcywp1da8nzfqsql2zqi648mfnx6qwkig3cv36n9m0yy676y";
      type = "gem";
    };
    version = "0.11.0";
  };
  oauth = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1m08365nyp0fgw2iyzj8q8qy8zml0c1hw2dd8cp82pp6656ahbh3";
      type = "gem";
    };
    version = "0.5.5";
  };
  rest-client = {
    dependencies = ["http-accept" "http-cookie" "mime-types" "netrc"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1qs74yzl58agzx9dgjhcpgmzfn61fqkk33k1js2y5yhlvc5l19im";
      type = "gem";
    };
    version = "2.1.0";
  };
  unf = {
    dependencies = ["unf_ext"];
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0bh2cf73i2ffh4fcpdn9ir4mhq8zi50ik0zqa1braahzadx536a9";
      type = "gem";
    };
    version = "0.1.4";
  };
  unf_ext = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "0wc47r23h063l8ysws8sy24gzh74mks81cak3lkzlrw4qkqb3sg4";
      type = "gem";
    };
    version = "0.0.7.7";
  };
  unicode = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1v8kxmq9i85agjpl7pnl72688901xhs8wxhmj6lpy16a8xz3nzxk";
      type = "gem";
    };
    version = "0.4.4.4";
  };
  unicode-display_width = {
    groups = ["default"];
    platforms = [];
    source = {
      remotes = ["https://rubygems.org"];
      sha256 = "1bilbnc8j6jkb59lrf177i3p1pdyxll0n8400hzqr35vl3r3kv2m";
      type = "gem";
    };
    version = "2.0.0";
  };
}
