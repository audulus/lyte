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
            url: "https://github.com/audulus/lyte/releases/download/v0.12/CLyte.xcframework.zip",
            checksum: "2527c5661f1647b8458f1515a5a80b47b6c86aabcfdfe556071c69c0a736c16c"
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
