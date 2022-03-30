{ mkDerivation, base, bindings-DSL, fetchFromGitHub, hsc2hs, lib
}:
mkDerivation {
  pname = "freetype2";
  version = "0.2";
  src = fetchFromGitHub {
    owner = "lamdu";
    repo = "freetype2";
    sha256 = "16kgbj9nqk6qvi7rgglzxnjccwq1qfyzk0kkzqlxa6d6cldmsswi";
    rev = "046328d92312d65638440132c016a3081d9c32a4";
  };
  libraryHaskellDepends = [ base ];
  librarySystemDepends = [ hsc2hs ];
  license = lib.licenses.bsd3;
  postInstall = ''
    mkdir $out/include
    cp freetype2/include/ft2build.h \
       freetype2/include/freetype/config/ftconfig.h \
       freetype2/include/freetype/ftgasp.h \
       freetype2/include/freetype/fterrors.h \
       freetype2/include/freetype/ftsystem.h \
       freetype2/include/freetype/ftmoderr.h \
       freetype2/include/freetype/ftrender.h \
       freetype2/include/freetype/ftchapters.h \
       freetype2/include/freetype/config/ftheader.h \
       freetype2/include/freetype/config/ftoption.h \
       freetype2/include/freetype/config/ftstdlib.h \
       freetype2/include/freetype/config/ftmodule.h \
       freetype2/include/freetype/config/ftconfig.h \
       freetype2/include/freetype/freetype.h \
       freetype2/include/freetype/ftwinfnt.h \
       freetype2/include/freetype/ftgzip.h \
       freetype2/include/freetype/ftbdf.h \
       freetype2/include/freetype/ftlist.h \
       freetype2/include/freetype/ftautoh.h \
       freetype2/include/freetype/tttables.h \
       freetype2/include/freetype/ftmm.h \
       freetype2/include/freetype/ftsizes.h \
       freetype2/include/freetype/fttrigon.h \
       freetype2/include/freetype/ftpfr.h \
       freetype2/include/freetype/ftotval.h \
       freetype2/include/freetype/ftstroke.h \
       freetype2/include/freetype/tttags.h \
       freetype2/include/freetype/ftsynth.h \
       freetype2/include/freetype/ftimage.h \
       freetype2/include/freetype/ftadvanc.h \
       freetype2/include/freetype/ftgxval.h \
       freetype2/include/freetype/ftbbox.h \
       freetype2/include/freetype/ftcache.h \
       freetype2/include/freetype/ftbitmap.h \
       freetype2/include/freetype/ftcid.h \
       freetype2/include/freetype/ftglyph.h \
       freetype2/include/freetype/ftoutln.h \
       freetype2/include/freetype/fterrdef.h \
       freetype2/include/freetype/internal/ftmemory.h \
       freetype2/include/freetype/internal/ftcalc.h \
       freetype2/include/freetype/internal/pshints.h \
       freetype2/include/freetype/internal/services/svgxval.h \
       freetype2/include/freetype/internal/services/svpfr.h \
       freetype2/include/freetype/internal/services/svkern.h \
       freetype2/include/freetype/internal/services/svttcmap.h \
       freetype2/include/freetype/internal/services/svbdf.h \
       freetype2/include/freetype/internal/services/svwinfnt.h \
       freetype2/include/freetype/internal/services/svprop.h \
       freetype2/include/freetype/internal/services/svgldict.h \
       freetype2/include/freetype/internal/services/svsfnt.h \
       freetype2/include/freetype/internal/services/svotval.h \
       freetype2/include/freetype/internal/services/svtteng.h \
       freetype2/include/freetype/internal/services/svttglyf.h \
       freetype2/include/freetype/internal/services/svpscmap.h \
       freetype2/include/freetype/internal/services/svcid.h \
       freetype2/include/freetype/internal/services/svpsinfo.h \
       freetype2/include/freetype/internal/services/svpostnm.h \
       freetype2/include/freetype/internal/services/svmm.h \
       freetype2/include/freetype/internal/services/svfntfmt.h \
       freetype2/include/freetype/internal/ftstream.h \
       freetype2/include/freetype/internal/fthash.h \
       freetype2/include/freetype/internal/ftvalid.h \
       freetype2/include/freetype/internal/tttypes.h \
       freetype2/include/freetype/internal/ftdebug.h \
       freetype2/include/freetype/internal/autohint.h \
       freetype2/include/freetype/internal/ftobjs.h \
       freetype2/include/freetype/internal/ftrfork.h \
       freetype2/include/freetype/internal/ftgloadr.h \
       freetype2/include/freetype/internal/fttrace.h \
       freetype2/include/freetype/internal/ftdriver.h \
       freetype2/include/freetype/internal/internal.h \
       freetype2/include/freetype/internal/psaux.h \
       freetype2/include/freetype/internal/t1types.h \
       freetype2/include/freetype/internal/sfnt.h \
       freetype2/include/freetype/internal/ftserv.h \
       freetype2/include/freetype/internal/ftpic.h \
       freetype2/include/freetype/ftcffdrv.h \
       freetype2/include/freetype/ftmodapi.h \
       freetype2/include/freetype/ftincrem.h \
       freetype2/include/freetype/t1tables.h \
       freetype2/include/freetype/ftlcdfil.h \
       freetype2/include/freetype/ftfntfmt.h \
       freetype2/include/freetype/ttunpat.h \
       freetype2/include/freetype/ttnameid.h \
       freetype2/include/freetype/ftlzw.h \
       freetype2/include/freetype/ftbzip2.h \
       freetype2/include/freetype/ftsnames.h \
       freetype2/include/freetype/ftttdrv.h \
       freetype2/include/freetype/fttypes.h       $out/include
  '';
}
