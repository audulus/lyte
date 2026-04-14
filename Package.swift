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
            url: "https://github.com/audulus/lyte/releases/download/0.24/CLyte.xcframework.zip",
            checksum: "4dd98784efcbe94afd7127134132ce3f08a3dafef2ec12bb3b9141372a452f0b"
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
