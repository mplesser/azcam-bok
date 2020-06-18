# Database entries for detectors.

detector_ccd57 = {
    "name": "CCD57",
    "description": "e2v CCD57",
    "ref_pixel": [256, 256],
    "format": [560, 24, 0, 0, 528, 14, 0, 0, 528],
    "focalplane": [1, 1, 1, 1, "0"],
    "roi": [1, 512, 1, 512, 1, 1],
    "extension_position": [[1, 1]],
    "jpg_order": [[1, 1]],
}

detector_ccid21 = {
    "name": "CCID21",
    "description": "MIT-LL CCID21",
    "ref_pixel": [256, 256],
    "format": [512, 4, 0, 0, 512, 0, 0, 0, 512],
    "focalplane": [1, 1, 1, 1, "0"],
    "roi": [1, 512, 1, 512, 1, 1],
    "extension_position": [[1, 1]],
    "jpg_order": [[1, 1]],
}

detector_ccid37 = {
    "name": "CCID37",
    "description": "MIT-LL CCID37",
    "ref_pixel": [256, 256],
    "format": [512, 4, 0, 0, 512, 0, 0, 0, 512],
    "focalplane": [1, 1, 1, 1, "0"],
    "roi": [1, 512, 1, 512, 1, 1],
    "extension_position": [[1, 1]],
    "jpg_order": [[1, 1]],
}

detector_512ft = {
    "name": "512ft",
    "description": "UA foundry 512FT CCD",
    "ref_pixel": [256, 256],
    "format": [512, 4, 0, 0, 512, 0, 0, 0, 512],
    "focalplane": [1, 1, 1, 1, "0"],
    "roi": [1, 512, 1, 512, 1, 1],
    "extension_position": [[1, 1]],
    "jpg_order": [1],
}

detector_sta0510 = {
    "name": "STA0510",
    "description": "STA STA0510 CCD",
    "ref_pixel": [600, 400],
    "format": [1200, 18, 0, 20, 800, 0, 0, 0, 0],
    "focalplane": [1, 1, 1, 1, "0"],
    "roi": [1, 1200, 1, 800, 1, 1],
    "extension_position": [[1, 1]],
    "jpg_order": [1],
}

detector_sta3800 = {
    "name": "STA3800",
    "description": "STA STA3800 CCD",
    "ref_pixel": [2036, 2000],
    "format": [509 * 8, 4, 0, 50, 2000 * 2, 0, 0, 10, 0],
    "focalplane": [1, 1, 8, 2, "0000000022222222"],
    "roi": [1, 4072, 1, 4000, 1, 1],
    "ampcfg": [0, 0, 0, 0, 0, 0, 0, 0, 2, 2, 2, 2, 2, 2, 2, 2],
    "extension_position": [
        [8, 1],
        [7, 1],
        [6, 1],
        [5, 1],
        [4, 1],
        [3, 1],
        [2, 1],
        [1, 1],
        [1, 2],
        [2, 2],
        [3, 2],
        [4, 2],
        [5, 2],
        [6, 2],
        [7, 2],
        [8, 2],
    ],
    "extnum": [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16],
    "jpg_order": [8, 7, 6, 5, 4, 3, 2, 1, 9, 10, 11, 12, 13, 14, 15, 16],
    "detnum": [1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1],
    "detpos": [
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
    ],
    "detgap": [
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
    ],
    "amp_position": [
        [1, 1],
        [2, 1],
        [3, 1],
        [4, 1],
        [5, 1],
        [6, 1],
        [7, 1],
        [8, 1],
        [1, 2],
        [2, 2],
        [3, 2],
        [4, 2],
        [5, 2],
        [6, 2],
        [7, 2],
        [8, 2],
    ],
    "amp_pixel_position": [
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
    ],
    "extname": [
        "im1",
        "im2",
        "im3",
        "im4",
        "im5",
        "im6",
        "im7",
        "im8",
        "im9",
        "im10",
        "im11",
        "im12",
        "im13",
        "im14",
        "im15",
        "im16",
    ],
}

detector_sta4400 = {
    "name": "STA4400",
    "description": "STA STA4400 CCD",
    "ref_pixel": [2036, 1000],
    "format": [509 * 8, 4, 0, 50, 2000, 0, 0, 10, 0],
    "focalplane": [1, 1, 8, 1, "00000000"],
    "roi": [1, 4072, 1, 2000, 1, 1],
    "ampcfg": [0, 0, 0, 0, 0, 0, 0, 0],
    "extension_position": [
        [1, 1],
        [2, 1],
        [3, 1],
        [4, 1],
        [5, 1],
        [6, 1],
        [7, 1],
        [8, 1],
    ],
    "extnum": [1, 2, 3, 4, 5, 6, 7, 8],
    "jpg_order": [1, 2, 3, 4, 5, 6, 7, 8],
    "detnum": [1, 1, 1, 1, 1, 1, 1, 1],
    "detpos": [[1, 1], [1, 1], [1, 1], [1, 1], [1, 1], [1, 1], [1, 1], [1, 1]],
    "detgap": [
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
        [0.0, 0.0],
    ],
    "amp_position": [[1, 1], [2, 1], [3, 1], [4, 1], [5, 1], [6, 1], [7, 1], [8, 1]],
    "amp_pixel_position": [
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
        [1, 1],
    ],
    "extname": ["im1", "im2", "im3", "im4", "im5", "im6", "im7", "im8"],
}

detector_sta3600 = {
    "name": "STA3600",
    "description": "STA STA3600 CCD",
    "ref_pixel": [1032, 1032],
    "format": [2064, 12, 0, 20, 2064, 0, 0, 0, 0],
    "focalplane": [1, 1, 1, 2, "03"],
    "roi": [1, 2064, 1, 2064, 1, 1],
    "extension_position": [[1, 1], [1, 2]],
    "jpg_order": [1, 2],
}
