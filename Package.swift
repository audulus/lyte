// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "Lyte",
    platforms: [.macOS(.v15), .iOS(.v16)],
    products: [
        .library(name: "Lyte",
        type: .dynamic, targets: ["Lyte"]),
    ],
    targets: [
        .binaryTarget(
            name: "CLyte",
            url: "https://github.com/audulus/lyte/releases/download/0.36/CLyte.xcframework.zip",
            checksum: "804c3fe2d676a0c583050a48e879c25b416a2c80ad4dbdeef2213f43e5926c0d"
        ),
        .target(
            name: "Lyte",
            dependencies: ["CLyte"],
            linkerSettings: [
                .linkedLibrary("c++"),
                .linkedLibrary("z"),
                .linkedLibrary("curses"),
            ]
        ),
        .testTarget(
            name: "LyteTests",
            dependencies: ["Lyte"]
        ),
    ]
)
