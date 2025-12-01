{
  "targets": [
    {
      "target_name": "jit_runtime",
      "sources": ["src/jit-runtime.c"],
      "include_dirs": [],
      "conditions": [
        ["OS=='mac'", {
          "xcode_settings": {
            "OTHER_CFLAGS": ["-arch", "arm64"],
            "OTHER_LDFLAGS": ["-arch", "arm64"]
          }
        }]
      ]
    }
  ]
}
