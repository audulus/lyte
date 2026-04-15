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
            url: "https://github.com/audulus/lyte/releases/download/0.26/CLyte.xcframework.zip",
            checksum: "1945cf13f39f0995efe32e69b07c4be60f35bdcba33e3c8d810bb372fe335996"
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
