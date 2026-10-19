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
            url: "https://github.com/audulus/lyte/releases/download/0.40.0/CLyte.xcframework.zip",
            checksum: "4fd76c27be9235e9a9c8ca4f944f49d796383cdf0be5540d55c5222c257c1685"
        ),
        .target(
            name: "Lyte",
            dependencies: ["CLyte"],
            linkerSettings: [
                .linkedLibrary("c++"),
                .linkedLibrary("z"),
                .linkedLibrary("curses"),
                // The macOS slices use the system libffi (see Cargo.toml).
                .linkedLibrary("ffi", .when(platforms: [.macOS])),
            ]
        ),
        .testTarget(
            name: "LyteTests",
            dependencies: ["Lyte"]
        ),
    ]
)
